;----------------------------------------------------------------
;
;   SlctClnIO.INC - 
;
;   Copyright(c) Names in the News Ca., Inc. 2002
;
;   Author: David L Herrick
;   Created: 2/7/2002 5:56:32 AM
;   Last change: DLH 2/7/2002 6:03:15 AM
;----------------------------------------------------------------

..............................................................................
.
. SlctClnIO INCLUSION
. NIN ADDITIONAL CHARGES FILE I/O ROUTINES
.
.
. FILE NAME : SelectClean.dat
. REC LENGTH: 70 Variable
. INDEX KEY : 1-35
.
..............................................................................
.
. ENTRY POINT : SlctClnKEY
. REQUIRED    : 'SlctClnFLD'
. RETURNED    : ADDITIONAL CHARGES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SlctClnKEY  BRANCH    SlctClnFLAG TO SlctCln1
            CALL      SlctClnOPEN
SlctCln1    FILEPI    1;SlctClnFILE
         READ      SlctClnFILE,SlctClnFLD;SlctClnVars
         RETURN
..............................................................................
.
. ENTRY POINT : SlctClnTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SlctClnTST  BRANCH    SlctClnFLAG TO SlctCln2
         CALL      SlctClnOPEN
SlctCln2    FILEPI    1;SlctClnFILE
         READ      SlctClnFILE,SlctClnFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : SlctClnKS
. REQUIRED    :
. RETURNED    : ADDITIONAL CHARGES RECORD
. DESCRIPTION : KEY SEQUENTIAL ADDITIONAL CHARGES FILE READ
.
SlctClnKS   BRANCH    SlctClnFLAG TO SlctCln3
         CALL      SlctClnOPEN
SlctCln3    FILEPI    1;SlctClnFILE
         READKS    SlctClnFILE;SlctClnVars
         RETURN
..............................................................................
.
. ENTRY POINT : SlctClnSEQ
. REQUIRED    :
. RETURNED    : ADDITIONAL CHARGES RECORD
. DESCRIPTION : SEQUENTIAL ADDITIONAL CHARGES FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SlctClnSEQ  BRANCH    SlctClnFLAG TO SlctCln4
         CALL      SlctClnOPEN
SlctCln4    FILEPI    1;SlctClnFILE
         READ      SlctClnFILE,SEQ;SlctClnVars
         RETURN
..............................................................................
.
. ENTRY POINT : SlctClnWRT
. REQUIRED    : 'SlctClnFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SlctClnWRT  BRANCH    SlctClnFLAG TO SlctCln5
            CALL      SlctClnOPEN
SlctCln5    FILEPI    1;SlctClnFILE
            WRITE     SlctClnFILE,SlctClnFLD;SlctClnVars
            RETURN
..............................................................................
.
. ENTRY POINT : SlctClnUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ADDITIONAL CHARGES FILE
.
SlctClnUPD  BRANCH    SlctClnFLAG TO SlctCln6
            CALL      SlctClnOPEN
SlctCln6    FILEPI    1;SlctClnFILE
            UPDATE    SlctClnFILE;SlctClnVars
            RETURN
..............................................................................
.
. ENTRY POINT : SlctClnDEL
. REQUIRED    : 'SlctClnFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SlctClnDEL  BRANCH    SlctClnFLAG TO SlctCln7
            CALL      SlctClnOPEN
SlctCln7     FILEPI    1;SlctClnFILE
             DELETE    SlctClnFILE,SlctClnFLD
             RETURN
..............................................................................
.
. ENTRY POINT : SlctClnOPEN
. REQUIRED    : 'SlctClnFLAG'
. RETURNED    : 'SlctClnFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN ADDITIONAL CHARGES FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SlctClnOPEN TRAP      SlctClnGONE IF IO
            OPEN      SlctClnFILE,SlctClnNAME
            TRAPCLR   IO
            MOVE      C1 TO SlctClnFLAG
            RETURN
.
SlctClnGONE MOVE      SlctClnNAME TO FILENAME
            CALL      FILEGONE
.

