;----------------------------------------------------------------
;
;   MrktLstio.inc -
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David L Herrick
;   Created: 3/12/2002 8:59:58 AM
;   Last change: DLH 3/18/2002 12:33:21 PM
;----------------------------------------------------------------

..............................................................................
.
. MrktIO INCLUSION
.
..............................................................................
.
. ENTRY POINT : MrktKEY
. REQUIRED    : 'MrktFLD'
. RETURNED    : Record
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MrktKEY  BRANCH    MrktFLAG TO Mrkt1
         CALL      MrktOPEN
Mrkt1    FILEPI    1;MrktFILE2
         READ      MrktFILE2,MrktFLD;MrktVARS
         RETURN
..............................................................................
.
. ENTRY POINT : MrktTST
. REQUIRED    : MrktFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
MrktTST  BRANCH    MrktFLAG TO Mrkt2
         CALL      MrktOPEN
Mrkt2    FILEPI    1;MrktFILE
         READ      MrktFILE2,MrktFLD;;
         RETURN
..............................................................................
.
. ENTRY POINT : MrktKS
. REQUIRED    :
. RETURNED    : RECORD
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
MrktKS   BRANCH    MrktFLAG TO Mrkt3
         CALL      MrktOPEN
Mrkt3    FILEPI    1;MrktFILE2
         READKS    MrktFILE2;MrktVARS
         RETURN
..............................................................................
.
. ENTRY POINT : MrktSEQ
. REQUIRED    :
. RETURNED    : SCHEDULE RECORD
. DESCRIPTION : SEQUENTIAL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MrktSEQ	 BRANCH    MrktFLAG TO Mrkt4
         CALL      MrktOPEN
Mrkt4    FILEPI    1;MrktFILE2
         READ      MrktFILE2,SEQ;Mrktvars

         RETURN
..............................................................................
.
. ENTRY POINT : MrktWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION :
.
MrktWRT  BRANCH    MrktFLAG TO Mrkt5
         CALL      MrktOPEN
Mrkt5    FILEPI    1;MrktFList
         WRITE     MrktFlist;Mrktvars

         RETURN
..............................................................................
.
. ENTRY POINT : MrktUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : UPDATE FILE
.
MrktUPD  BRANCH    MrktFLAG TO Mrkt6
         CALL      MrktOPEN
Mrkt6    FILEPI    1;MrktFList
         UPDATE    MrktFList;Mrktvars

         RETURN
..............................................................................
.
. ENTRY POINT : MrktDEL
. REQUIRED    : Previous Valid Read
. RETURNED    :
. DESCRIPTION : EXACT KEY DELETE
.
MrktDEL  BRANCH    MrktFLAG TO Mrkt7
         CALL      MrktOPEN
Mrkt7     FILEPI    1;MrktFList
         DELETE    MrktFList
         RETURN
...............................................................................
.
. ENTRY POINT : MrktOPEN
. REQUIRED    : 'MrktFLAG'
. RETURNED    : 'MrktFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
MrktOPEN TRAP      MrktGONE IF IO
         OPEN      MrktFList
         TRAPCLR   IO
         MOVE      C1 TO MrktFLAG
         RETURN
.
MrktGONE MOVE      MrktNAME1 TO FILENAME
         CALL      FILEGONE
.
..............................................................................

