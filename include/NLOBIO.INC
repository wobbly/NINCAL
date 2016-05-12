..............................................................................
.
. NlobIO INCLUSION
. NIN list owner balance FILE I/O ROUTINES
.
.
.  FILE NAME : NINlob
. REC LENGTH: 28 FIXED
. INDEX KEY : 1-16
..............................................................................
.
. ENTRY POINT : NlobKEY
. REQUIRED    : 'NlobFLD'
. RETURNED    : RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NlobKEY  BRANCH    NlobFLAG TO Nlob1
         CALL      NlobOPEN
Nlob1    FILEPI    1;NlobFILE
         READ      NlobFILE,NlobFLD;lobLON:
                                    loblist:
                                    lobCC:
                                    lobYY:
                                    lobMM:
                                    lobbal 
         RETURN
..............................................................................
.
. ENTRY POINT : NlobTST
. REQUIRED    : NlobFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NlobTST  BRANCH    NlobFLAG TO Nlob2
         CALL      NlobOPEN
Nlob2    FILEPI    1;NlobFILE
         READ      NlobFILE,NlobFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NlobKS
. REQUIRED    :
. RETURNED    : lobER RECORD
. DESCRIPTION : KEY SEQUENTIAL lobER FILE READ
.
NlobKS   BRANCH    NlobFLAG TO Nlob3
         CALL      NlobOPEN
Nlob3    FILEPI    1;NlobFILE
         READKS      NlobFILE;lobLON:
                                    loblist:
                                    lobCC:
                                    lobYY:
                                    lobMM:
                                    lobbal 
         RETURN
..............................................................................
. ENTRY POINT : NlobSEQ
. REQUIRED    :
. RETURNED    : lobER RECORD
. DESCRIPTION : SEQUENTIAL lobER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NlobSEQ 
Nlob4A	 BRANCH    NlobFLAG TO Nlob4B
         CALL      NlobOPEN
Nlob4B   FILEPI    1;NlobFILE
         READ      NlobFILE,SEQ;lobLON:
                                    loblist:
                                    lobCC:
                                    lobYY:
                                    lobMM:
                                    lobbal 
         RETURN
..............................................................................
.
. ENTRY POINT : NlobWRT
. REQUIRED    : 'NlobFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NlobWRT  BRANCH    NlobFLAG TO Nlob5
         CALL      NlobOPEN
Nlob5    FILEPI    1;NlobFILE
         WRITE      NlobFILE,NlobFLD;lobLON:
                                    loblist:
                                    lobCC:
                                    lobYY:
                                    lobMM:
                                    lobbal 
         RETURN
..............................................................................
.
. ENTRY POINT : NlobUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE lobER FILE
.
NlobUPD  BRANCH    NlobFLAG TO Nlob6
         CALL      NlobOPEN
Nlob6    FILEPI    1;NlobFILE
         UPDATE    NlobFILE;lobLON:
                                    loblist:
                                    lobCC:
                                    lobYY:
                                    lobMM:
                                    lobbal 
         RETURN
..............................................................................
.
. ENTRY POINT : NlobDEL
. REQUIRED    : 'NlobFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NlobDEL  BRANCH    NlobFLAG TO Nlob7
         CALL      NlobOPEN
Nlob7    FILEPI    1;NlobFILE
         DELETE    NlobFILE,NlobFLD
         RETURN
..............................................................................
. 
. ENTRY POINT : NlobOPEN
. REQUIRED    : 'NlobFLAG' 'NlobPATH'
. RETURNED    : 'NlobFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN lobER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NlobOPEN 
NlobOPN1 TRAP      NlobGONE IF IO
         OPEN      NlobFILE,NlobNAME
         TRAPCLR   IO
         MOVE      C1 TO NlobFLAG
         RETURN
..............................................................................
NlobGONE MOVE      NlobNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

