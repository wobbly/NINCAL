;----------------------------------------------------------------
;
;   Nprmio.inc - 
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David L Herrick
;   Created: 3/12/2002 8:59:58 AM
;   Last change: DLH 3/12/2002 8:59:58 AM
;----------------------------------------------------------------

..............................................................................
.
. PrmIO INCLUSION
.
..............................................................................
.
. ENTRY POINT : PrmKEY
. REQUIRED    : 'PrmFLD'
. RETURNED    : Record
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
PrmKEY  BRANCH    PrmFLAG TO Prm1
         CALL      PrmOPEN
Prm1    FILEPI    1;PrmFILE
         READ      PrmFILE2,PrmFLD;PrmVARS
         RETURN
..............................................................................
.
. ENTRY POINT : PrmTST
. REQUIRED    : PrmFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
PrmTST  BRANCH    PrmFLAG TO Prm2
         CALL      PrmOPEN
Prm2    FILEPI    1;PrmFILE
         READ      PrmFILE2,PrmFLD;;
         RETURN
..............................................................................
.
. ENTRY POINT : PrmKS
. REQUIRED    :
. RETURNED    : RECORD
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
PrmKS   BRANCH    PrmFLAG TO Prm3
         CALL      PrmOPEN
Prm3    FILEPI    1;PrmFILE
         READKS    PrmFILE2;PrmVARS
         RETURN
..............................................................................
.
. ENTRY POINT : PrmSEQ
. REQUIRED    :
. RETURNED    : SCHEDULE RECORD
. DESCRIPTION : SEQUENTIAL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
PrmSEQ	 BRANCH    PrmFLAG TO Prm4
         CALL      PrmOPEN
Prm4    FILEPI    1;PrmFILE
         READ      PrmFILE,SEQ;Prmvars

         RETURN
..............................................................................
.
. ENTRY POINT : PrmWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION :
.
PrmWRT  BRANCH    PrmFLAG TO Prm5
         CALL      PrmOPEN
Prm5    FILEPI    1;PrmFList
         WRITE     PrmFlist;Prmvars

         RETURN
..............................................................................
.
. ENTRY POINT : PrmUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : UPDATE FILE
.
PrmUPD  BRANCH    PrmFLAG TO Prm6
         CALL      PrmOPEN
Prm6    FILEPI    1;PrmFList
         UPDATE    PrmFList;Prmvars

         RETURN
..............................................................................
.
. ENTRY POINT : PrmDEL
. REQUIRED    : Previous Valid Read
. RETURNED    :
. DESCRIPTION : EXACT KEY DELETE
.
PrmDEL  BRANCH    PrmFLAG TO Prm7
         CALL      PrmOPEN
Prm7     FILEPI    1;PrmFList
         DELETE    PrmFList
         RETURN
...............................................................................
.
. ENTRY POINT : PrmOPEN
. REQUIRED    : 'PrmFLAG'
. RETURNED    : 'PrmFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
PrmOPEN TRAP      PrmGONE IF IO
         OPEN      PrmFList
         TRAPCLR   IO
         MOVE      C1 TO PrmFLAG
         RETURN
.
PrmGONE MOVE      PrmNAME1 TO FILENAME
         CALL      FILEGONE
.
..............................................................................

