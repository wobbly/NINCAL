..............................................................................
.
. StatXio INCLUSION
. StatX Stats FILE DEFINITION
.
. FILE NAME : StatXref
. REC LENGTH:  FIXED
. INDEX KEY : 
..............................................................................
.
..............................................................................
.
. ENTRY POINT : StatXKEY
. REQUIRED    : 'StatXFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
StatXKEY  BRANCH    Statxpath to Statx1a,Statx1b,Statx1c
Statx1a   BRANCH    StatXFLAG TO StatX1d
         CALL      StatXOPEN
StatX1d   FILEPI    1;StatXFILE
         READ      StatXFILE,StatXFLD;StatXVARS
         RETURN
Statx1b   BRANCH    StatXFLG1 TO StatX1e
         CALL      StatXOPEN
StatX1e   FILEPI    1;StatXFLE1
         READ      StatXFLE1,StatXFLD1;StatXVARS
         RETURN
Statx1c   BRANCH    StatXFLG2 TO StatX1f
         CALL      StatXOPEN
StatX1f   FILEPI    1;StatXFLE2
         READ      StatXFLE2,StatXFLD2;StatXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : StatXTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
StatXTST  BRANCH    StatXFLAG TO StatX2
         CALL      StatXOPEN
StatX2    FILEPI    1;StatXFILE
         READ      StatXFILE,StatXFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : StatXKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
StatXKS   BRANCH    StatXFLAG TO StatX3
         CALL      StatXOPEN
StatX3    FILEPI    1;StatXFILE
         READKS    StatXFILE;StatXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : StatXSEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
StatXSEQ  BRANCH    StatXFLAG TO StatX4
         CALL      StatXOPEN
StatX4    FILEPI    1;StatXFILE
         READ      StatXFILE,SEQ;StatXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : StatXWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
StatXWRT BRANCH    StatXFLAG OF StatX5d
         CALL      StatXOPEN
StatX5d   FILEPI    1;StatXFLst
         WRITE     StatXFLst;STATXvars
         RETURN
..............................................................................
.
. ENTRY POINT : StatXUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
StatXUPD  BRANCH    StatXFLAG TO StatX6
         CALL      StatXOPEN
StatX6    FILEPI    1;StatXFLst
         UPDATE    StatXFLst;STATXvars
         RETURN
..............................................................................
.
. ENTRY POINT : StatXDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
StatXDEL  BRANCH    StatXFLAG TO StatX7
         CALL      StatXOPEN
StatX7    FILEPI    1;StatXFLSt
         DELETE    StatXFLST
         RETURN
..............................................................................
.
. ENTRY POINT : StatXOPEN
. REQUIRED    : 'StatXFLAG'
. RETURNED    : 'StatXFLAG' SET TO '1' IF OPENNED
. DESCRIPTION :  open all files associated with the file list
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
StatXOPEN TRAP      StatXGONE IF IO

         OPEN      StatXFLst
         TRAPCLR   IO
         MOVE      C1 TO StatXFLAG
         MOVE      C1 TO StatXFLG1
         MOVE      C1 TO StatXFLG2
         RETURN
.
StatXGONE MOVE      StatXNAME TO FILENAME
         CALL      FILEGONE
.

