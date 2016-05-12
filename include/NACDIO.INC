..............................................................................
.
. NACDIO INCLUSION
. NIN ADDITIONAL CHARGES FILE I/O ROUTINES
.
.
. FILE NAME : NINACD
. REC LENGTH: 65 FIXED
. INDEX KEY : 1-3 (3 POSITION ACD#)
.
.patch 2.01    DLH 05Nov2003
.patch 2.0     DLH Apr99  3 byte key/code
..............................................................................
.
. ENTRY POINT : NACDKEY
. REQUIRED    : 'NACDFLD'
. RETURNED    : ADDITIONAL CHARGES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NACDKEY  BRANCH    NACDFLAG TO NACD1
         CALL      NACDOPEN
NACD1    FILEPI    1;NACDFILE
         READ      NACDFILE,NACDFLD;NACDKEY:     001-003   KEY
                                    NACDTEXT:    004-038   ADD CHARGES DESC
                                    NACDFILL1:   039-043   NOT USED
                                    NacdType:    044-044        ' ' = special, 'm' = per thousand 'f' = flat
                                    NacdFill2    045-065
                                    RETURN

..............................................................................
.
. ENTRY POINT : NACDTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NACDTST  BRANCH    NACDFLAG TO NACD2
         CALL      NACDOPEN
NACD2    FILEPI    1;NACDFILE
         READ      NACDFILE,NACDFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NACDKS
. REQUIRED    :
. RETURNED    : ADDITIONAL CHARGES RECORD
. DESCRIPTION : KEY SEQUENTIAL ADDITIONAL CHARGES FILE READ
.
NACDKS   BRANCH    NACDFLAG TO NACD3
         CALL      NACDOPEN
NACD3    FILEPI    1;NACDFILE
         READKS    NACDFILE;NACDKEY:     001-003   KEY
                                    NACDTEXT:    004-038   ADD CHARGES DESC
                                    NACDFILL1:   039-043   NOT USED
                                    NacdType:    044-044        ' ' = special, 'm' = per thousand 'f' = flat
                                    NacdFill2    045-065
                                    RETURN
..............................................................................
.
. ENTRY POINT : NACDSEQ
. REQUIRED    :
. RETURNED    : ADDITIONAL CHARGES RECORD
. DESCRIPTION : SEQUENTIAL ADDITIONAL CHARGES FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NACDSEQ  BRANCH    NACDFLAG TO NACD4
         CALL      NACDOPEN
NACD4    FILEPI    1;NACDFILE
         READ      NACDFILE,SEQ;NACDKEY:     001-003   KEY
                                    NACDTEXT:    004-038   ADD CHARGES DESC
                                    NACDFILL1:   039-043   NOT USED
                                    NacdType:    044-044        ' ' = special, 'm' = per thousand 'f' = flat
                                    NacdFill2    045-065 
                                       RETURN
..............................................................................
.
. ENTRY POINT : NACDWRT
. REQUIRED    : 'NACDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NACDWRT  BRANCH    NACDFLAG TO NACD5
         CALL      NACDOPEN
NACD5    FILEPI    1;NACDFILE
         WRITE     NACDFILE,NACDFLD;NACDKEY:     001-003   KEY
                                    NACDTEXT:    004-038   ADD CHARGES DESC
                                    NACDFILL1:   039-043   NOT USED
                                    NacdType:    044-044        ' ' = special, 'm' = per thousand 'f' = flat
                                    NacdFill2    045-065 
         RETURN
..............................................................................
.
. ENTRY POINT : NACDUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ADDITIONAL CHARGES FILE
.
NACDUPD  BRANCH    NACDFLAG TO NACD6
         CALL      NACDOPEN
NACD6    FILEPI    1;NACDFILE
         UPDATE    NACDFILE;NACDKEY:     001-003   KEY
                                    NACDTEXT:    004-038   ADD CHARGES DESC
                                    NACDFILL1:   039-043   NOT USED
                                    NacdType:    044-044        ' ' = special, 'm' = per thousand 'f' = flat
                                    NacdFill2    045-065 
         RETURN
..............................................................................
.
. ENTRY POINT : NACDDEL
. REQUIRED    : 'NACDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NACDDEL  BRANCH    NACDFLAG TO NACD7
         CALL      NACDOPEN
NACD7    FILEPI    1;NACDFILE
         DELETE    NACDFILE,NACDFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NACDOPEN
. REQUIRED    : 'NACDFLAG'
. RETURNED    : 'NACDFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN ADDITIONAL CHARGES FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NACDOPEN      TRAP      NACDGONE IF IO
              OPEN      NACDFILE,NACDNAME
              TRAPCLR   IO
              TRAP           IOmssg GIVING ERROR IF IO
         
              MOVE      C1 TO NACDFLAG
              RETURN
.
NACDGONE MOVE      NACDNAME TO FILENAME
         CALL      FILEGONE
.

