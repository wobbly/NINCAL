..............................................................................
.
. TNCBAKIO INCLUSION
.
. FILE NAME : TNCBAK
. REC LENGTH: 754
. INDEX KEY : 1-4 
..............................................................................
.
. ENTRY POINT : TNCBAKKEY
. REQUIRED    : 'TNCBAKFLD'
. RETURNED    : BROKER NOTES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
TNCBAKKEY BRANCH    TNCBAKFLAG TO TNCBAK1
         CALL      TNCBAKOPEN
         
TNCBAK1   trap      IOMssg giving Error if IO
         branch    TNCBAKlock of TNCBAK1L,TNCBAK1R,TNCBAK1N
.default FIlepi's active
TNCBAK1L  FILEPI    1;TNCBAKFILE
         READ      TNCBAKFILE,TNCBAKFLD;TNCBAKVARS
         trapclr   IO
         RETURN
.with record locks . note file open must have had flag set
TNCBAK1R
         READLK    TNCBAKFILE,TNCBAKFLD;TNCBAKVARS
         trapclr  IO
         RETURN
.No locks
TNCBAK1N
         READ      TNCBAKFILE,TNCBAKFLD;TNCBAKVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKTST
. REQUIRED    : TNCBAKFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
TNCBAKTST BRANCH    TNCBAKFLAG TO TNCBAK2
         CALL      TNCBAKOPEN
TNCBAK2   trap      IOMssg giving Error if IO
         FILEPI    1;TNCBAKFILE
         READ      TNCBAKFILE,TNCBAKFLD;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKKS
. REQUIRED    :
. RETURNED    : BROKER NOTES RECORD
. DESCRIPTION : KEY SEQUENTIAL BRKCIAL INSTRUCTION FILE READ
.
TNCBAKKS  BRANCH    TNCBAKFLAG TO TNCBAK3
         CALL      TNCBAKOPEN
TNCBAK3   trap      IOMssg giving Error if IO
         BRANCH    TNCBAKLOCK OF TNCBAK3L,TNCBAK3R,TNCBAK3N

TNCBAK3L  FILEPI    1;TNCBAKFILE
         READKS    TNCBAKFILE;TNCBAKVARS
         trapclr   IO
         RETURN
TNCBAK3R
         READKSLK  TNCBAKFILE;TNCBAKVARS
         trapclr   IO
         RETURN
TNCBAK3N
         READKS    TNCBAKFILE;TNCBAKVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKSEQ
. REQUIRED    :
. RETURNED    : BROKER NOTES RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
TNCBAKSEQ BRANCH    TNCBAKFLAG TO TNCBAK4
         CALL      TNCBAKOPEN
TNCBAK4   trap      IOMssg giving Error if IO
         BRANCH    TNCBAKLOCK OF TNCBAK4L,TNCBAK4R,TNCBAK4N
TNCBAK4L  FILEPI    1;TNCBAKFILE
         READ      TNCBAKFILE,SEQ;TNCBAKVARS
         trapclr   IO
         RETURN
TNCBAK4R
         READLK    TNCBAKFILE,SEQ;TNCBAKVARS
         trapclr   IO
         RETURN
TNCBAK4N
         READ      TNCBAKFILE,SEQ;TNCBAKVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKWRT
. REQUIRED    : TNCBAKFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
TNCBAKWRT BRANCH    TNCBAKFLAG OF TNCBAK5
         CALL      TNCBAKOPEN
TNCBAK5   FILEPI    1;TNCBAKFILE
         WRITE     TNCBAKFILE,TNCBAKFLD;TNCBAKVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKUPD
. REQUIRED    : PREVIOUS READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI UPDATE
.
TNCBAKUPD BRANCH    TNCBAKFLAG OF TNCBAK6
         CALL      TNCBAKOPEN
TNCBAK6   FILEPI    1;TNCBAKFILE
         UPDATE    TNCBAKFILE;TNCBAKVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKDEL
. REQUIRED    : TNCBAKFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
TNCBAKDEL BRANCH    TNCBAKFLAG TO TNCBAK7
         CALL      TNCBAKOPEn
TNCBAK7   trap      IOMssg giving Error if IO
         FILEPI    1;TNCBAKFILE
         DELETE    TNCBAKFILE,TNCBAKFLD
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TNCBAKOPEN
. REQUIRED    : 'TNCBAKFLAG'
. RETURNED    : 'TNCBAKFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
TNCBAKOPEN TRAP     TNCBAKGONE IF IO
         BRANCH    TNCBAKLOCK TO TNCBAKOPNL,TNCBAKOPNR

TNCBAKOPNL OPEN     TNCBAKFILE,TNCBAKNAME
         TRAPCLR   IO
         MOVE      C1 TO TNCBAKFLAG
         RETURN
TNCBAKOPNR OPEN     TNCBAKFILE,TNCBAKNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO TNCBAKFLAG
         RETURN

.
TNCBAKGONE MOVE     TNCBAKNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
