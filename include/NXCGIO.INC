..............................................................................
.
. NxcgIO INCLUSION
.
.       PATCH #1.0 - 11-09-1999 ASH CENTURY ADDED, QTY EXPANDED
..............................................................................
.
. ENTRY POINT : NxcgKEY
. REQUIRED    : 'NxcgFLD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NxcgKEY 
         BRANCH    NxcgFLAG TO Nxcg1
         CALL      NxcgOPEN
Nxcg1    FILEPI    1;EXCHARGE
.START PATCH 1.0 - REPLACED LOGIC
.         READ      EXCHARGE,NxcgFLD;nXCGLR:        1-6   KEY= LR Position 1-6
.                            NXCGMLR:               7-10  MAILER
.                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
.                            NXCGLN:               14-19   LIST NUMBER
.                            NXCGOWN:              20-23   OWNER
.                            NXCGTPI:              24-29
.                            NXCGQTY:              30-36   QUANTITY
.                            NXCGAR:               37-47   AR
.                            NXCGAP:               48-58   AP
.                            NXCGYR:               59-60   YEAR
.                            NXCGMO                61-62   MONTH
         READ      EXCHARGE,NxcgFLD;nXCGLR:        1-6   KEY= LR Position 1-6
                            NXCGMLR:               7-10  MAILER
                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
                            NXCGLN:               14-19   LIST NUMBER
                            NXCGOWN:              20-23   OWNER
                            NXCGTPI:              24-29
                            NXCGQTY:              30-38   QUANTITY
                            NXCGAR:               39-49   AR
                            NXCGAP:               50-60   AP
                            nxcgcc:               61-62   CENTURY
                            NXCGYR:               63-64   YEAR
                            NXCGMO                65-66   MONTH
.END PATCH 1.0 - REPLACED LOGIC
         RETURN
.
..............................................................................
.
. ENTRY POINT : NxcgTST
. REQUIRED    : NxcgFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NxcgTST  
         BRANCH    NxcgFLAG TO Nxcg2
         CALL      NxcgOPEN
Nxcg2    FILEPI    1;EXCHARGE
         READ      EXCHARGE,NxcgFLD;STR1
         RETURN
.
..............................................................................
.
. ENTRY POINT : NxcgKS
. REQUIRED    :
.
NxcgKS   
         BRANCH    NxcgFLAG TO Nxcg3
         CALL      NxcgOPEN
Nxcg3    FILEPI    1;EXCHARGE
.START PATCH 1.0 - REPLACED LOGIC
.         READKS    EXCHARGE;nXCGLR:                1-6   KEY= LR Position 1-6
.                            NXCGMLR:               7-10  MAILER
.                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
.                            NXCGLN:               14-19   LIST NUMBER
.                            NXCGOWN:              20-23   OWNER
.                            NXCGTPI:              24-29
.                            NXCGQTY:              30-36   QUANTITY
.                            NXCGAR:               37-47   AR
.                            NXCGAP:               48-58   AP
.                            NXCGYR:               59-60   YEAR
.                            NXCGMO                61-62   MONTH
         READKS    EXCHARGE;nXCGLR:                1-6   KEY= LR Position 1-6
                            NXCGMLR:               7-10  MAILER
                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
                            NXCGLN:               14-19   LIST NUMBER
                            NXCGOWN:              20-23   OWNER
                            NXCGTPI:              24-29
                            NXCGQTY:              30-38   QUANTITY
                            NXCGAR:               39-49   AR
                            NXCGAP:               50-60   AP
                            nxcgcc:               61-62   CENTURY
                            NXCGYR:               63-64   YEAR
                            NXCGMO                65-66   MONTH
.END PATCH 1.0 - REPLACED LOGIC
         RETURN
.
..............................................................................
.
. ENTRY POINT : NxcgSEQ
. REQUIRED    :
.
NxcgSEQ  
         BRANCH    NxcgFLAG TO Nxcg4
         CALL      NxcgOPEN
Nxcg4    FILEPI    1;EXCHARGE
.START PATCH 1.0 - REPLACED LOGIC
.         READ      EXCHARGE,SEQ;nXCGLR:            1-6   KEY= LR Position 1-6
.                            NXCGMLR:               7-10  MAILER
.                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
.                            NXCGLN:               14-19   LIST NUMBER
.                            NXCGOWN:              20-23   OWNER
.                            NXCGTPI:              24-29
.                            NXCGQTY:              30-36   QUANTITY
.                            NXCGAR:               37-47   AR
.                            NXCGAP:               48-58   AP
.                            NXCGYR:               59-60   YEAR
.                            NXCGMO                61-62   MONTH
         READ      EXCHARGE,SEQ;nXCGLR:            1-6   KEY= LR Position 1-6
                            NXCGMLR:               7-10  MAILER
                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
                            NXCGLN:               14-19   LIST NUMBER
                            NXCGOWN:              20-23   OWNER
                            NXCGTPI:              24-29
                            NXCGQTY:              30-38   QUANTITY
                            NXCGAR:               39-49   AR
                            NXCGAP:               50-60   AP
                            nxcgcc:               61-62   CENTURY
                            NXCGYR:               63-64   YEAR
                            NXCGMO                65-66   MONTH
.END PATCH 1.0 - REPLACED LOGIC
         RETURN
.
..............................................................................
.
. ENTRY POINT : NxcgWRT
. REQUIRED    : 'NxcgFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NxcgWRT  BRANCH    NxcgFLAG TO Nxcg5
         CALL      NxcgOPEN
Nxcg5    FILEPI    1;EXCHARGE
.START PATCH 1.0 - REPLACED LOGIC
.         WRITE     EXCHARGE,NxcgFLD;nXCGLR:        1-6   KEY= LR Position 1-6
.                            NXCGMLR:               7-10  MAILER
.                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
.                            NXCGLN:               14-19   LIST NUMBER
.                            NXCGOWN:              20-23   OWNER
.                            NXCGTPI:              24-29
.                            NXCGQTY:              30-36   QUANTITY
.                            NXCGAR:               37-47   AR
.                            NXCGAP:               48-58   AP
.                            NXCGYR:               59-60   YEAR
.                            NXCGMO                61-62   MONTH
         WRITE     EXCHARGE,NxcgFLD;nXCGLR:        1-6   KEY= LR Position 1-6
                            NXCGMLR:               7-10  MAILER
                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
                            NXCGLN:               14-19   LIST NUMBER
                            NXCGOWN:              20-23   OWNER
                            NXCGTPI:              24-29
                            NXCGQTY:              30-38   QUANTITY
                            NXCGAR:               39-49   AR
                            NXCGAP:               50-60   AP
                            nxcgcc:               61-62   CENTURY
                            NXCGYR:               63-64   YEAR
                            NXCGMO                65-66   MONTH
.END PATCH 1.0 - REPLACED LOGIC
         RETURN
..............................................................................
.
. ENTRY POINT : NxcgUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST- MAILER XREF FILE
.
NxcgUPD  BRANCH    NxcgFLAG TO Nxcg6
         CALL      NxcgOPEN
Nxcg6    FILEPI    1;EXCHARGE
.START PATCH 1.0 - REPLACED LOGIC
.         UPDATE    EXCHARGE;nXCGLR:        1-6   KEY= LR Position 1-6
.                            NXCGMLR:               7-10  MAILER
.                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
.                            NXCGLN:               14-19   LIST NUMBER
.                            NXCGOWN:              20-23   OWNER
.                            NXCGTPI:              24-29
.                            NXCGQTY:              30-36   QUANTITY
.                            NXCGAR:               37-47   AR
.                            NXCGAP:               48-58   AP
.                            NXCGYR:               59-60   YEAR
.                            NXCGMO                61-62   MONTH
         UPDATE    EXCHARGE;nXCGLR:        1-6   KEY= LR Position 1-6
                            NXCGMLR:               7-10  MAILER
                            NXCGMCT:              11-13  MAILER/CONTACT NUMBER.
                            NXCGLN:               14-19   LIST NUMBER
                            NXCGOWN:              20-23   OWNER
                            NXCGTPI:              24-29
                            NXCGQTY:              30-38   QUANTITY
                            NXCGAR:               39-49   AR
                            NXCGAP:               50-60   AP
                            nxcgcc:               61-62   CENTURY
                            NXCGYR:               63-64   YEAR
                            NXCGMO                65-66   MONTH
.END PATCH 1.0 - REPLACED LOGIC
         RETURN
..............................................................................
.
. ENTRY POINT : NxcgDEL
. REQUIRED    : 'NxcgFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NxcgDEL  BRANCH    NxcgFLAG TO Nxcg7
         CALL      NxcgOPEN
Nxcg7    FILEPI    1;EXCHARGE
         DELETE    EXCHARGE,NxcgFLD
         RETURN
...............................................................................
.
. ENTRY POINT : NxcgOPEN
. REQUIRED    : 'NxcgPATH'
. RETURNED    : 'NxcgFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN LIST- MAILER XREF FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NxcgOPEN 
         TRAP      NxcgGONE IF IO
         OPEN      EXCHARGE,NxcgNAME
         TRAPCLR   IO
         MOVE      C1 TO NxcgFLAG
         RETURN
.
.
NxcgGONE MOVE      NxcgNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
