...............................................................................
..
.. BatchIO INCLUSION
.. NIN Batch  FILE I/O ROUTINES
..
...............................................................................
..
.. ENTRY POINT : BatchKEY
.. REQUIRED    : 'BatchFLD'
.. RETURNED    : Contact RECORD
.. DESCRIPTION : EXACT ISAM KEY READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..
BatchKEY
          BRANCH    BatchFLAG,Batch1
          CALL      BatchOPEN
Batch1
          TRAP      IOMssg Giving Error if IO
          BRANCH    BatchLOCK,Batch1L,Batch1R,Batch1N
Batch1L
          FILEPI    1;BatchFILE
          READ      BatchFILE,BatchFLD;BatchVARS
          RETURN
Batch1R    
          READLK    BatchFILE,BatchFLD;BatchVARS
          RETURN
Batch1N
          READ      BatchFILE,BatchFLD;BatchVARS
          RETURN
.. ENTRY POINT : BatchKEY2
.. REQUIRED    : 'BatchFLD'
.. RETURNED    : Contact RECORD
.. DESCRIPTION : EXACT ISAM KEY READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
BatchKEY2
          BRANCH    BatchFLAG,Batch2
          CALL      BatchOPEN
Batch2
          TRAP      IOMssg Giving Error if IO
          BRANCH    BatchLOCK,Batch2l,Batch2r,Batch2n
Batch2L
          FILEPI    1;BatchFILE2
          READ      BatchFILE2,BatchFLD4;BatchVARS
          RETURN
Batch2R
          READLK    BatchFILE2,BatchFLD4;BatchVARS
          RETURN
Batch2N
          READ      BatchFILE2,BatchFLD4;BatchVARS
          RETURN
...............................................................................
..
.. ENTRY POINT : BatchTST
.. REQUIRED    : BatchFLD
.. RETURNED    :
.. DESCRIPTION : TEST KEY
..
BatchTST
          BRANCH    BatchFLAG,Batch2T
          CALL      BatchOPEN
Batch2T
          TRAP      IOMssg Giving Error if IO
          BRANCH    BatchLOCK,Batch2LT,Batch2RT,Batch2NT
Batch2LT
          FILEPI    1;BatchFILE
          READ      BatchFILE,BatchFLD;;
          RETURN
Batch2RT
          READLK    BatchFILE,BatchFLD;;
          RETURN
Batch2NT
          READ      BatchFILE,BatchFLD;;
          RETURN
.patchdb
...............................................................................
..
.. ENTRY POINT : BatchTST2
.. REQUIRED    : BatchFLD
.. RETURNED    :
.. DESCRIPTION : TEST KEY
..
BatchTST2
          BRANCH    BatchFLAG,Batch2T2
          CALL      BatchOPEN
Batch2T2
          TRAP      IOMssg Giving Error if IO
          BRANCH    BatchLOCK,Batch2LT2,Batch2RT2,Batch2NT2
Batch2LT2
          FILEPI    1;BatchFILE
          READ      BatchFile2,BatchFLD4;;
          RETURN
Batch2RT2
          READLK    BatchFile2,BatchFLD4;;
          RETURN
Batch2NT2
          READ      BatchFile2,BatchFLD4;;
          RETURN
...............................................................................
...............................................................................
..
.. ENTRY POINT : BatchKS
.. REQUIRED    :
.. RETURNED    : Contact RECORD
.. DESCRIPTION : KEY SEQUENTIAL Contact FILE READ
..
BatchKS
          BRANCH    BatchFLAG,Batch3
          CALL      BatchOPEN
Batch3
          TRAP      IOMssg Giving Error if IO
          BRANCH    BatchLOCK,Batch3L,Batch3R,Batch3N
Batch3L
          FILEPI    1;BatchFILE
          READKS    BatchFILE;BatchVARS
          RETURN
Batch3R
          READKSLK BatchFILE;BatchVARS
          RETURN
Batch3N
          READKS    BatchFILE;BatchVARS
          RETURN
...............................................................................
.. ENTRY POINT : BatchSEQ
.. REQUIRED    :
.. RETURNED    : Contact RECORD
.. DESCRIPTION : SEQUENTIAL Contact FILE READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLGS
..
BatchSEQ
          BRANCH    BatchFLAG,Batch4
          CALL      BatchOPEN
Batch4
          TRAP      IOMssg Giving Error if IO
          BRANCH    BatchLOCK,Batch4L,Batch4R,Batch4N
Batch4L
          FILEPI    1;BatchFILE
          READ      BatchFILE,SEQ;BatchVARS
          RETURN
Batch4r
          READLK    BatchFILE,SEQ;BatchVARS
          RETURN
Batch4N
          READ      BatchFILE,SEQ;BatchVARS
          RETURN
...............................................................................
..
.. ENTRY POINT : BatchWRT
.. REQUIRED    : 'BatchFLD'
.. RETURNED    :
.. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
..
BatchWRT
          BRANCH    BatchFLAG,Batch5
          CALL      BatchOPEN
Batch5
          TRAP      IOMssg Giving Error if IO
          FILEPI    1;BatchFLIST
          WRITE     BatchFLIST;BatchVARS
          RETURN
...............................................................................
..
.. ENTRY POINT : BatchUPD
.. REQUIRED    : A PREVIOUS KEY READ
.. RETURNED    :
.. DESCRIPTION : KEY UPDATE Contact FILE
..
BatchUPD
          BRANCH    BatchFLAG,Batch6
          CALL      BatchOPEN
Batch6
          TRAP      IOMssg Giving Error if IO
          FILEPI    1;BatchFLIST
          UPDATE    BatchFLIST;BatchVARS
          RETURN
...............................................................................
..
.. ENTRY POINT : BatchDEL
.. REQUIRED    : 'BatchFLD'
.. RETURNED    :
.. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
..
BatchDEL
          BRANCH    BatchFLAG,Batch7
          CALL      BatchOPEN
Batch7
          TRAP      IOMssg Giving Error if IO
          FILEPI    1;BatchFLIST
          DELETE    BatchFLIST
          RETURN
..............................................................................
...............................................................................
.. 
.. ENTRY POINT : BatchOPEN
.. REQUIRED    : 'BatchFLAG' 'BatchPATH'
.. RETURNED    : 'BatchFLAG' SET TO '1' IF OPENNED
.. DESCRIPTION : OPEN NIN Contact FILE
..               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
..
BatchOPEN
          TRAP      BatchGONE IF IO
          OPEN      BatchFLIST
          TRAPCLR   IO
          MOVE      C1,BatchFLAG
          RETURN
...............................................................................
BatchGONE
          MOVE      BatchNAME,FILENAME
          CALL      FILEGONE
..
...............................................................................
