..............................................................................
.
. NBILIO INCLUSION
. NIN BILL-TO FILE I/O ROUTINES
.
. FILE NAME : NINBIL
. REC LENGTH: 130 FIXED
. INDEX KEY : 2-9 (4 POSITION MLR#, 3 POSITION CONTACT#, 1 POSITION BILL#)
.
..............................................................................
.
. ENTRY POINT : NBILKEY
. REQUIRED    : 'NBILFLD'
. RETURNED    : BILL-TO RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBILKEY  BRANCH    NBILFLAG TO NBIL1
         CALL      NBILOPEN
NBIL1    FILEPI    1;NBILFILE
         READ      NBILFILE,NBILFLD;BILFIL1:     001-001   FILLER
                                    BILMLR:      002-005   MAILER COMPANY NUMBE
                                    BILCNT:      006-008   MAILER CONTACT NUMBE
                                    BILCODE:     009-009   BILL-TO NUMBER
                                    BILNAME:     010-034   CONTACT NAME
                                    BILCOMP:     035-059   COMPANY NAME
                                    BILADDR:     060-084   ADDRESS
                                    BILCITY:     085-099   CITY
                                    BILSTATE:    100-101   STATE
                                    BILZIP:      102-111   ZIP CODE
                                    BILUSER:     112-121   LAST USER TO MODIFY
                                    BILRVDTE:    122-127   REVISED DATE
                                    BILFIL3      128-130   FILLER
         RETURN
..............................................................................
.
. ENTRY POINT : NBILTST
. REQUIRED    : NBILFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NBILTST  BRANCH    NBILFLAG TO NBIL2
         CALL      NBILOPEN
NBIL2    FILEPI    1;NBILFILE
         READ      NBILFILE,NBILFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NBILKS
. REQUIRED    :
. RETURNED    : BILL-TO RECORD
. DESCRIPTION : KEY SEQUENTIAL BILL-TO FILE READ
.
NBILKS   BRANCH    NBILFLAG TO NBIL3
         CALL      NBILOPEN
NBIL3    FILEPI    1;NBILFILE
         READKS    NBILFILE;BILFIL1:     001-001   FILLER
                            BILMLR:      002-005   MAILER COMPANY NUMBER
                            BILCNT:      006-008   MAILER CONTACT NUMBER
                            BILCODE:     009-009   BILL-TO NUMBER
                            BILNAME:     010-034   CONTACT NAME
                            BILCOMP:     035-059   COMPANY NAME
                            BILADDR:     060-084   ADDRESS
                            BILCITY:     085-099   CITY
                            BILSTATE:    100-101   STATE
                            BILZIP:      102-111   ZIP CODE
                            BILUSER:     112-121   LAST USER TO MODIFY
                            BILRVDTE:    122-127   REVISED DATE
                            BILFIL3      128-130   FILLER
         RETURN
..............................................................................
.
. ENTRY POINT : NBILSEQ
. REQUIRED    :
. RETURNED    : BILL-TO RECORD
. DESCRIPTION : SEQUENTIAL BILL-TO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..  ******UPDATED 18MAR92 - FOR SEPERATE SEQ FILE DESCRIPTOR, NBILPATH=1 USE 
.                          ISAM FILE, 3=USE FLAT FILE.

NBILSEQ  BRANCH    NBILPATH TO NBIL4A,NBILGONE,NBIL4C
NBIL4A   BRANCH    NBILFLAG TO NBIL4B
         CALL      NBILOPEN
NBIL4B   FILEPI    1;NBILFILE
         READ      NBILFILE,SEQ;BILFIL1:     001-001   FILLER
                                BILMLR:      002-005   MAILER COMPANY NUMBER
                                BILCNT:      006-008   MAILER CONTACT NUMBER
                                BILCODE:     009-009   BILL-TO NUMBER
                                BILNAME:     010-034   CONTACT NAME
                                BILCOMP:     035-059   COMPANY NAME
                                BILADDR:     060-084   ADDRESS
                                BILCITY:     085-099   CITY
                                BILSTATE:    100-101   STATE
                                BILZIP:      102-111   ZIP CODE
                                BILUSER:     112-121   LAST USER TO MODIFY
                                BILRVDTE:    122-127   REVISED DATE
                                BILFIL3      128-130   FILLER
         RETURN
NBIL4C   BRANCH    NBILFLG3 TO NBIL4D
         CALL      NBILOPEN
NBIL4D   FILEPI    1;NBILFLE3
         READ      NBILFLE3,SEQ;BILFIL1:     001-001   FILLER
                                BILMLR:      002-005   MAILER COMPANY NUMBER
                                BILCNT:      006-008   MAILER CONTACT NUMBER
                                BILCODE:     009-009   BILL-TO NUMBER
                                BILNAME:     010-034   CONTACT NAME
                                BILCOMP:     035-059   COMPANY NAME
                                BILADDR:     060-084   ADDRESS
                                BILCITY:     085-099   CITY
                                BILSTATE:    100-101   STATE
                                BILZIP:      102-111   ZIP CODE
                                BILUSER:     112-121   LAST USER TO MODIFY
                                BILRVDTE:    122-127   REVISED DATE
                                BILFIL3      128-130   FILLER
         RETURN
..............................................................................
.
. ENTRY POINT : NBILWRT
. REQUIRED    : 'NBILFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NBILWRT  BRANCH    NBILFLAG TO NBIL5
         CALL      NBILOPEN
NBIL5    FILEPI    1;NBILFILE
         WRITE     NBILFILE,NBILFLD;BILFIL1:     001-001   FILLER
                                    BILMLR:      002-005   MAILER COMPANY NUMBE
                                    BILCNT:      006-008   MAILER CONTACT NUMBE
                                    BILCODE:     009-009   BILL-TO NUMBER
                                    BILNAME:     010-034   CONTACT NAME
                                    BILCOMP:     035-059   COMPANY NAME
                                    BILADDR:     060-084   ADDRESS
                                    BILCITY:     085-099   CITY
                                    BILSTATE:    100-101   STATE
                                    BILZIP:      102-111   ZIP CODE
                                    BILUSER:     112-121   LAST USER TO MODIFY
                                    BILRVDTE:    122-127   REVISED DATE
                                    BILFIL3      128-130   FILLER
         RETURN
..............................................................................
.
. ENTRY POINT : NBILUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE BILL-TO FILE
.
NBILUPD  BRANCH    NBILFLAG TO NBIL6
         CALL      NBILOPEN
NBIL6    FILEPI    1;NBILFILE
         UPDATE    NBILFILE;BILFIL1:     001-001   FILLER
                            BILMLR:      002-005   MAILER COMPANY NUMBER
                            BILCNT:      006-008   MAILER CONTACT NUMBER
                            BILCODE:     009-009   BILL-TO NUMBER
                            BILNAME:     010-034   CONTACT NAME
                            BILCOMP:     035-059   COMPANY NAME
                            BILADDR:     060-084   ADDRESS
                            BILCITY:     085-099   CITY
                            BILSTATE:    100-101   STATE
                            BILZIP:      102-111   ZIP CODE
                            BILUSER:     112-121   LAST USER TO MODIFY
                            BILRVDTE:    122-127   REVISED DATE
                            BILFIL3      128-130   FILLER
         RETURN
..............................................................................
.
. ENTRY POINT : NBILDEL
. REQUIRED    : 'NBILFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NBILDEL  BRANCH    NBILFLAG TO NBIL7
         CALL      NBILOPEN
NBIL7    FILEPI    1;NBILFILE
         DELETE    NBILFILE,NBILFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NBILAIM
. REQUIRED    : 'NBILFLD'
. RETURNED    : BILL-TO RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBILAIM  BRANCH    NBILFLG2 TO NBIL8
         CALL      NBILOPN2
NBIL8    FILEPI    1;NBILFIL2
         READ      NBILFIL2,NBILFLD1;BILFIL1:     001-001   FILLER
                                    BILMLR:      002-005   MAILER COMPANY NUMBE
                                    BILCNT:      006-008   MAILER CONTACT NUMBE
                                    BILCODE:     009-009   BILL-TO NUMBER
                                    BILNAME:     010-034   CONTACT NAME
                                    BILCOMP:     035-059   COMPANY NAME
                                    BILADDR:     060-084   ADDRESS
                                    BILCITY:     085-099   CITY
                                    BILSTATE:    100-101   STATE
                                    BILZIP:      102-111   ZIP CODE
                                    BILUSER:     112-121   LAST USER TO MODIFY
                                    BILRVDTE:    122-127   REVISED DATE
                                    BILFIL3      128-130   FILLER
         RETURN
..............................................................................
.
. ENTRY POINT : NBILKG
. REQUIRED    : A PREVIOS VALID AIM READ
. RETURNED    : BILL-TO RECORD
. DESCRIPTION : KEY GENERIC AIM READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBILKG  BRANCH    NBILFLG2 TO NBIL9
         CALL      NBILOPN2
NBIL9    FILEPI    1;NBILFIL2
         READKG      NBILFIL2;BILFIL1:     001-001   FILLER
                                    BILMLR:      002-005   MAILER COMPANY NUMBE
                                    BILCNT:      006-008   MAILER CONTACT NUMBE
                                    BILCODE:     009-009   BILL-TO NUMBER
                                    BILNAME:     010-034   CONTACT NAME
                                    BILCOMP:     035-059   COMPANY NAME
                                    BILADDR:     060-084   ADDRESS
                                    BILCITY:     085-099   CITY
                                    BILSTATE:    100-101   STATE
                                    BILZIP:      102-111   ZIP CODE
                                    BILUSER:     112-121   LAST USER TO MODIFY
                                    BILRVDTE:    122-127   REVISED DATE
                                    BILFIL3      128-130   FILLER
         RETURN
...............................................................................
.
. ENTRY POINT : NBILOPEN
. REQUIRED    : 'NBILFLAG'
. RETURNED    : 'NBILFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN BILL-TO FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NBILOPEN BRANCH    NBILPATH OF NBILOPN1,NBILOPN2,NBILOPN3
NBILOPN1 TRAP      NBILGONE IF IO
         OPEN      NBILFILE,NBILNAME
         TRAPCLR   IO
         MOVE      C1 TO NBILFLAG
         RETURN
NBILOPN2 TRAP      NBILGONE IF IO
         OPEN      NBILFIL2,NBILNAME
         TRAPCLR   IO
         MOVE      C1 TO NBILFLG2
         RETURN
NBILOPN3 TRAP      NBILGONE IF IO
         OPEN      NBILFLE3,NBILNAME
         TRAPCLR   IO
         MOVE      C1 TO NBILFLG3
         RETURN
NBILGONE MOVE      NBILNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
